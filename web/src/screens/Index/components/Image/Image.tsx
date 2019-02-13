import React       from 'react';
import styles      from './Image.module.css';
import mergeStyles from '../../../../utilities/mergeStyles';

interface Props {
    empty?: boolean;
    resolution?: string;
}

const Image = (props: Props) => {
    return (
        <div className={mergeStyles(styles.Root, props.empty && 'empty')}>
            <span className={styles.Resolution}>
                {props.resolution}
            </span>
        </div>
    );
}

export default Image;
