import React      from 'react';
import classnames from 'classnames';
import styles     from './Image.module.css';

interface Props {
    contain?: boolean;
    empty?: boolean;
    path?: string;
    resolution?: string;
    width: number;
}

const classes = (props: Props) => classnames({
    [styles.Root]: true,
    [styles.Root__empty]: props.empty,
    [styles.Root__contained]: props.contain
})

const Image = (props: Props) => {
    const styleOverrides = {
        backgroundImage: `url(${props.path})`,
        height: `${props.width}px`,
        width: `${props.width}px`,
    };

    return (
        <div style={styleOverrides} className={classes(props)}>
            <span className={styles.Resolution}>
                {props.resolution}
            </span>
        </div>
    );
}

export default Image;
