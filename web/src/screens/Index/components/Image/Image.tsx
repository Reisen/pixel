import React       from 'react';
import styles      from './Image.module.css';

interface Props {
    empty?: boolean;
    path?: string;
    resolution?: string;
    width: number;
}

const Image = (props: Props) => {
    const styleOverrides = {
        backgroundImage: `url(${props.path})`,
        height: `${props.width}px`,
        width: `${props.width}px`,
    };

    return (
        <div
            style={styleOverrides}
            className={props.empty ? styles.Root__empty : styles.Root} >

            <span className={styles.Resolution}>
                {props.resolution}
            </span>
        </div>
    );
}

export default Image;
