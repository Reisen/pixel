import React  from 'react';
import styles from './Image.module.css';

interface Props {
    empty?: boolean;
    resolution?: string;
}

const Image = (props: Props) => {
    return (
        <div className={styles.Root}>
            <span className={styles.Resolution}>
                {props.resolution}
            </span>
        </div>
    );
}

export default Image;
